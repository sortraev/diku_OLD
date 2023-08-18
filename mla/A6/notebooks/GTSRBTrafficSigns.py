import os
import torchvision
from torchvision import datasets, transforms
from torch.utils.data.dataset import Dataset
from torchvision.datasets.utils import download_url, extract_archive

class GTSRBTrafficSigns(Dataset):
    """ GTSRB data set """

    def __init__(self,
                 root = './',
                 url = 'https://sid.erda.dk/share_redirect/EB0rrpZwuI',
                 filename='EB0rrpZwuI.zip',
                 train = True,
                 force_download = False,
                 crop_size = 28):
        """
        Parameters:
        root: str
            Parent directory
        url: str
            URL of data set
        filename: str
            Filename of the zipped data
        train: bool
            Indicating whether training or test data should be considered
        force_download: bool
            Download data from server even if a local copy is detected
        crop_size: int
            Height and width of the input image
        """
        self.img_height = 32
        self.img_width  = self.img_height
        self.img_height_crop = crop_size
        self.img_width_crop  = self.img_height_crop

        self.train = train
        archive = os.path.join(root, filename)

        self.data_folder = os.path.join(root, 'GTSRB/train' if self.train else 'GTSRB/test')

        if (not os.path.exists(self.data_folder)) or force_download:
            download_url(url, root, filename)
            extract_archive(archive, root, False)
        else:
            print('Using existing', self.data_folder)

        self.dataset_train = datasets.ImageFolder(self.data_folder)

    def __getitem__(self, index):
        image, label = self.dataset_train[index]
        image = transforms.Resize((self.img_width,self.img_height))(image)

        # The transformations just serve as examples
        if self.train:
            image = transforms.RandomAffine((-5,5))(image)
            image = transforms.RandomCrop((self.img_width_crop, self.img_height_crop))(image)
            image = transforms.ColorJitter(0.8, contrast = 0.4)(image)
            if label in [11, 12, 13, 17, 18, 26, 30, 35]:
                image = transforms.RandomHorizontalFlip(p = 0.5)(image)
        else:
            image = transforms.CenterCrop((self.img_width_crop, self.img_height_crop))(image)

        image = transforms.ToTensor()(image)

        return image, label

    def __len__(self):
        return len(self.dataset_train)
